! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>&Uuml;bertragsrechner</h2>
!! @author Guntram Sei&szlig;, Susanne Spohr
!! @version 1.21 vom 03/23/07, Quellcode: mod_b_carry.f90
!! <HR>
!! Basic type and methods for carry calculations based on integer components.  <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-05-02 : Guntram Sei&szlig;, Susanne Spohr : abgeleitet von carry_calculation <BR>
!  01.21 : 2007-03-23 : G. Lang : neu add_carry, sub_carry, mul_carry, div_carry, rnd_carry, double_to_carry
!                                 Unterprogramme ans Stelle Funktionen -> kein unnoetiges Allokieren
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Das Modul stellt einen sehr allgemein gefassten &Uuml;bertragsrechner<BR>
!! dar (carry ist die englische &Uuml;bersetzung von &Uuml;bertrag!).   <BR>
!! Das Modul stellt den Datentyp "t_carry" und Methoden zur Ver-        <BR>
!! f&uuml;gung, welch den Aufbau INTEGER-basierter Rechensysteme        <BR>
!! mit automatischem &Uuml;bertrag erlauben. So kann aufbauend auf      <BR>
!! diesem Modul unter anderem eine Zeitrechnung in Tagen, Stunden,      <BR>
!! Minuten und Sekunden oder ein exaktes Rechensystem f&uuml;r          <BR>
!! KiloEuro, Euro, Cent und MilliCent entwickelt werden.                <BR>
!! Die Grenzen, an denen ein Rechen&uuml;bertrag erfolgt, k&ouml;nnen   <BR>
!! vom Programmierer frei gew&auml;hlt werden.                          <BR>
!! <HR>
!!                                                                  <BR>
!! <A href="b_carry.html#links"<B><FONT COLOR="red" SIZE=14> [Zu den Links]</FONT></B></A>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_carry" <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> values : Enth&auml;lt die Werte einer Rechenzahl, beispielsweise : <BR>
!!                   values(1)=KiloEuro, values(2)=Euro, values(3)=Cent ...
!!     <LI> limits : Enth&auml;lt die Werte einer Rechenzahl, f&uuml;r obiges Beispiel:
!!                   limits(1)=1000000, limits(2)=1000, limits(3)=100  ...
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_carry mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_carry mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   <BR>
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,  <BR>
!!          ob das Modul korrekt initialisert wurde (ok_initialised)  <BR>
!!                                                                    <BR>
!! <HR>
!! Allgemein             [  0000 bis  0999 ]                        <BR>
!! 00000 = kein Fehler                                              <BR> 
!! 00001 = Modul ist nicht initialisiert.                           <BR>
!! 00002 = Modul ist schon initialisiert.                           <BR>
!! <HR>
!! INIT-Methoden         [  1000 bis  1999 ]                        <BR>
!! <HR>
!! CLEAR-Methoden        [  2000 bis  2999 ]                        <BR>
!! <HR>
!! SETUP-Methoden        [  3000 bis  3999 ]                        <BR>
!! <HR>
!! NEW-Methoden          [  4000 bis  4999 ]                        <BR>
!! <HR>
!! KILL-Methoden         [  5000 bis  5999 ]                        <BR>
!! 05010 = De-Allokieren der Komponente values des Typs t_carry   <BR>
!! 05020 = De-Allokieren der Komponente limits des Typs t_carry   <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! 06010 = Fehler in Komponente values des Typs t_carry             <BR>
!! 06020 = Fehler in Komponente limits des Typs t_carry             <BR>
!! 06030 = Feldgroesse der Komponenten unterschiedlich              <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken der Komponente values des Typs t_carry           <BR>
!! 07020 = Drucken der Komponente limits des Typs t_carry           <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! 08010 = Allokieren von Komponente values des Typs t_carry      <BR>
!! 08020 = Allokieren von Komponente limits des Typs t_carry      <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! <HR>
!! OPERATOR(==)-Methoden [ 10000 bis 10999 ]                        <BR>
!! <HR>
!! OPERATOR(+)-Methoden  [ 11000 bis 11999 ]                        <BR>
!! <HR>
!! OPERATOR(-)-Methoden  [ 12000 bis 12999 ]                        <BR>
!! <HR>
!! OPERATOR(*)-Methoden  [ 13000 bis 13999 ]                        <BR>
!! <HR>
!! OPERATOR(/)-Methoden  [ 14000 bis 14999 ]                        <BR>
!! <HR>
!! OPERATOR(>)-Methoden  [ 15000 bis 15999 ]                        <BR>
!! <HR>
!! OPERATOR(>=)-Methoden [ 16000 bis 16999 ]                        <BR>
!! <HR>
!! OPERATOR(<)-Methoden  [ 17000 bis 17999 ]                        <BR>
!! <HR>
!! OPERATOR(<=)-Methoden [ 18000 bis 18999 ]                        <BR>
!! <HR>
!! OPERATOR(/=)-Methoden [ 19000 bis 19999 ]                        <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!! <A NAME="links"> </A>
MODULE b_carry
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       long,         &
       double
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
  !
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       new_error,           &
       kill_error,          &
       print_error,         &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! <kurze Beschreibung des Typs angeben>  
  !! values : Integer-Komponenten der einzelnen Bereiche
  !! limits : die oberen Grenzen der einzelnen Bereiche
  TYPE , PUBLIC :: t_carry
     PRIVATE
     INTEGER (kind=long) , POINTER, DIMENSION(:) :: values 
     INTEGER , POINTER, DIMENSION(:) :: limits 
  END TYPE t_carry
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! Hinweis: verschiedene Methoden arbeiten auf Skalar und 1D-Array.
  !          Ggf. weitere ergaenzen (z.B. 2D-Array) falls sinnvoll.
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls.
  INTERFACE init_carry
     MODULE PROCEDURE init_carry_d ! 
  END INTERFACE
  !
  !! De-Allokieren/Re-Initialisieren der statischen Datenobjekte des Moduls.
  INTERFACE clear_carry
     MODULE PROCEDURE clear_carry_d ! 
  END INTERFACE
  !
  !! Logische Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden auf
  !! Benutzerwert setzen. <BR>
  !! Keine Ausgabe, wenn <EM>PRN_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>PRN_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_carry_prn_lun
     MODULE PROCEDURE setup_carry_prn_lun_d ! 
  END INTERFACE
  !
  !! Logische Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden auf
  !! Benutzerwert setzen. <BR>
  !! Keine Ausgabe, wenn <EM>TRC_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>TRC_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_carry_trc_lun
     MODULE PROCEDURE setup_carry_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen eines neuen Objektes vom Typ "t_carry" (Skalar, 1D-Array). <BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder. <BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_carry
     MODULE PROCEDURE new_carry_0  ! Version fuer Skalar
     MODULE PROCEDURE new_carry_1  ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Vernichten von Datenobjekten "t_carry" (Skalar, 1D-Array).<BR>
  !! Ggf. De-Allokieren von Memory.<BR>
  !! Teilweise Re-Initialisieren mit Default-Werten.
  INTERFACE kill_carry
     MODULE PROCEDURE kill_carry_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_carry_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Pr&uuml;fen von Datenobjekten "t_carry" auf G&uuml;ltigkeit (Skalar, 1D-Array). <BR>
  !! Aufrufe: <BR>
  !! a) valid = ok_carry(this)<BR>
  !! b) valid(:) = ok_carry(this(:))
  INTERFACE ok_carry
     MODULE PROCEDURE ok_carry_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_carry_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller Komponenten von Datenobjekten "t_carry" auf <EM>PRN_LUN</EM>
  !! (Skalar, 1D-Array). <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_carry
     MODULE PROCEDURE print_carry_0 ! Version fuer Skalar
     MODULE PROCEDURE print_carry_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten auf <EM>PRN_LUN</EM>. <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_carry_static
     MODULE PROCEDURE print_carry_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls auf <EM>PRN_LUN</EM>. <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_carry_all_errors
     MODULE PROCEDURE print_carry_all_errors_d ! 
  END INTERFACE
  !
  !! Globaler Setter, der gleichzeitig die Komponente "values" und 
  !! die Komponente "limits" belegt.<BR>
  !! Es erfolgt in jedem Fall eine Anpassung der "values" an die
  !! &uuml;bergebenen oberen Grenzen.<BR>
  !! Ggf. Allokieren von Memory zur Aufnahme der Benutzer-Daten.<BR>
  !! Aufrufe:<BR>
  !! a) CALL set_carry(this, ifeld(:), ilimits(:))<BR>
  !! b) CALL set_carry(this(:), ifeld(:), ilimits(:))<BR>
  !! <EM>Hinweis:</EM> das erste &uuml;bergebene Feld ist vom Typ INTEGER (KIND=long),
  !! das zweite vom Typ INTEGER, TARGET !
  INTERFACE set_carry
     MODULE PROCEDURE set_carry_0_1_IT1
     MODULE PROCEDURE set_carry_1_1_IT1
  END INTERFACE
  !
  !! Setze Komponente "values" in "t_carry" auf Benutzerwert.<BR>
  !! Ist bereits die Komponente "limits" gesetzt, erfolgt zus&auml;tzlich
  !! die Anpassung der Werte an die aktuellen oberen Grenzen.<BR>
  !! Ggf. Allokieren von Memory zur Aufnahme der Benutzer-Daten.<BR>
  !! Aufrufe:<BR>
  !! a) CALL set_carry_limits(this, ifeld(:))<BR>
  !! b) CALL set_carry_limits(this(:), ifeld(:))<BR>
  !! <EM>Hinweis:</EM> das &uuml;bergebene Feld ist vom Typ INTEGER (KIND=long) !
  INTERFACE set_carry_values
     MODULE PROCEDURE set_carry_values_0_1 ! Objekt (Skalar) / Daten (Vektor)
     MODULE PROCEDURE set_carry_values_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !
  !! Setze Komponente "limits" in "t_carry" auf Benutzerwert.<BR>
  !! Passt die Werte von "values" an "limits" an falls m&ouml;glich.<BR>
  !! Aufrufe:<BR>
  !! a) CALL set_carry_limits(this, ifeld(:))<BR>
  !! b) CALL set_carry_limits(this(:), ifeld(:))
  !! <EM>Hinweis:</EM> das &uuml;bergebene Feld ist vom Typ INTEGER, TARGET !
  INTERFACE set_carry_limits
     MODULE PROCEDURE set_carry_limits_0_IT1 ! Objekt (Skalar) / Targetfeld
     MODULE PROCEDURE set_carry_limits_1_IT1 ! Objekt (Vektor) / Targetfeld
  END INTERFACE
  !
  !! Hole Komponente "values" aus "t_carry".
  INTERFACE get_carry_values
     MODULE PROCEDURE get_carry_values_0_1 ! Skalar
  END INTERFACE
  !
  !! Hole Komponente "limits" aus "t_carry".
  INTERFACE get_carry_limits
     MODULE PROCEDURE get_carry_limits_0_1 ! Skalar
  END INTERFACE
  !
  !! Bestimme das globale Vorzeichen eines Objektes vom Typ "t_carry".
  INTERFACE get_carry_sign
     MODULE PROCEDURE get_carry_sign_0 ! Skalar
     MODULE PROCEDURE get_carry_sign_1 ! Vektor
  END INTERFACE
  !
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Runde den Wert eines Carry-Objektes als Unterprogramm <BR>
  INTERFACE rnd_carry
     MODULE PROCEDURE rnd_carry_0_0_0
  END INTERFACE
  !
  !! Runde den Wert eines Carry-Objektes. <BR>
  !! Diese Methode erwartet die Angabe des Feldindex, bis zu dem der Wert
  !! des Objektes "t_carry" gerundet werden soll. Wird beispielsweise
  !! der Index 2 &uuml;bergeben, so wird der dritte "values"-Wert auf
  !! 0 gesetzt, falls der Wert < 0.5 limits(3) ist, sonst auf limits(3).<BR>
  !! Anschliessend wird ein &Uuml;bertrag gemacht falls n&ouml;tig.
  INTERFACE round_carry
     MODULE PROCEDURE round_carry_0 ! 
     MODULE PROCEDURE round_carry_1 ! 
  END INTERFACE
  !
  !! Konvertiere den Wert eines Carry-Objektes in eine Real-Zahl.<BR>
  !! Diese Methode erwartet die Angabe des Feldindex, der bestimmt, welcher
  !! "values"-Wert eins zu eins nach REAL (KIND=double) &uuml;bertragen wird.
  INTERFACE carry_to_real
     MODULE PROCEDURE carry_to_real_0 ! 
     MODULE PROCEDURE carry_to_real_1 ! 
  END INTERFACE
  !
  !! Konvertiere eine Double-Zahl in ein Carry-Pbjekt als Unterprogramm <BR>
  !! a) Double in skalares Objekt
  INTERFACE double_to_carry
     MODULE PROCEDURE double_to_carry_0_dp
  END INTERFACE
  !
  !! Konvertiere eine Real-Zahl in ein Carry-Objekt.<BR>
  !! Diese Methode erwartet die Angabe des Feldindex, der bestimmt, welcher
  !! "values"-Wert eins zu eins dem Wert INT( REAL (KIND=double)) entspricht.
  INTERFACE real_to_carry
     MODULE PROCEDURE real_to_carry_0 ! 
     MODULE PROCEDURE real_to_carry_1 ! 
  END INTERFACE
  !
  !! R&uuml;ckgabe des Absolutwertes des Objektes vom Typ "t_carry".
  INTERFACE abs_carry
     MODULE PROCEDURE absolute_value !
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_carry" auf Gleichheit. <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:<BR>
  !! a) 2 Objekte des Typs "t_carry"<BR>
  !! b) 1 Objekt des Typs "t_carry" mit einem Feld von Objekten des Typs "t_carry"<BR>
  !! c) 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"<BR>
  !! d) 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Addition zweier Datenobjekte "t_carry" als Unterprogramm  <BR>
  !! a) Skalar + Skalar
  INTERFACE add_carry
     MODULE PROCEDURE add_carry_0_0_0
  END INTERFACE
  !
  !! Addition zweier Datenobjekte "t_carry". <BR>
  !! Die "values"-Felder der Operanden werden addiert und anschlie&szlig;end
  !! ein &Uuml;bertrag durchgef&uuml;hrt.
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE ad_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ad_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ad_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ad_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Subtraktion zweier Datenobjekte "t_carry" als Unterprogramm  <BR>
  !! a) Skalar + Skalar
  INTERFACE sub_carry
     MODULE PROCEDURE sub_carry_0_0_0
  END INTERFACE
  !
  !! Subtraktion zweier Datenobjekte "t_carry". <BR>
  !! Die "values"-Felder der Operanden werden subtrahiert und anschlie&szlig;end
  !! ein &Uuml;bertrag durchgef&uuml;hrt.
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE su_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE su_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE su_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE su_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Multiplikation mit Datenobjekten "t_carry" als Unterprogramm <BR>
  !! a) Integer mit Skalar <BR>
  !! b) Double mit Skalar
  INTERFACE mul_carry
     MODULE PROCEDURE mul_carry_0_0_in
     MODULE PROCEDURE mul_carry_0_0_dp
  END INTERFACE
  !
  !! Multiplikation Datenobjekte "t_carry" mit einem Faktor. <BR>
  !! M&ouml;glich ist die Multiplikation mit INTEGER und
  !! REAL(KIND=double). Es k&ouml;nnen Felder des Objektes "t_carry"
  !! mit einem Faktor multipliziert werden. <BR>
  !! Die "values"-Felder der Operanden werden mit einem Faktor multipliziert
  !! und anschlie&szlig;end ein &Uuml;bertrag durchgef&uuml;hrt.
  INTERFACE OPERATOR(*)
     MODULE PROCEDURE mu_carry_I0_0  ! Integer / Skalar
     MODULE PROCEDURE mu_carry_0_I0  ! Skalar  / Integer
     MODULE PROCEDURE mu_carry_I0_1  ! Integer / Vektor
     MODULE PROCEDURE mu_carry_1_I0  ! Vektor  / Integer
     MODULE PROCEDURE mu_carry_D0_0  ! Double  / Skalar
     MODULE PROCEDURE mu_carry_0_D0  ! Skalar  / Double
     MODULE PROCEDURE mu_carry_D0_1  ! Double  / Vektor
     MODULE PROCEDURE mu_carry_1_D0  ! Vektor  / Double
     ! vielleicht spaeter noch realisieren :
     ! MODULE PROCEDURE mu_carry_R0_0  ! Real   / Skalar
     ! MODULE PROCEDURE mu_carry_0_R0  ! Skalar / Real
     ! MODULE PROCEDURE mu_carry_R0_1  ! Real   / Vektor
     ! MODULE PROCEDURE mu_carry_1_R0  ! Vektor / Real
  END INTERFACE
  !
  !! Multiplikation mit Datenobjekten "t_carry" als Unterprogramm <BR>
  !! a) Integer mit Skalar
  INTERFACE div_carry
     MODULE PROCEDURE div_carry_0_0_in
  END INTERFACE
  !
  !! Division Datenobjekte "t_carry" durch Integer-Faktor. <BR>
  !! Berechnet den i-ten Teil des Objektes.<BR>
  !! Dabei wird ausschlie&szlig;lich die "values"-Komponente modifiziert.
  INTERFACE OPERATOR(/)
     MODULE PROCEDURE di_carry_0_I0  ! Skalar / Integer
     MODULE PROCEDURE di_carry_1_I0  ! Vektor / Integer
     ! vielleicht spaeter noch realisieren :
     ! MODULE PROCEDURE di_carry_0_D0  ! Skalar / Double
     ! MODULE PROCEDURE di_carry_1_D0  ! Vektor / Double
     ! MODULE PROCEDURE di_carry_0_R0  ! Skalar / Real
     ! MODULE PROCEDURE di_carry_1_R0  ! Vektor / Real
     ! MODULE PROCEDURE di_carry_0_0  ! Skalar / Skalar - Res: INTEGER
     ! MODULE PROCEDURE di_carry_1_0  ! Vektor / Skalar
     ! MODULE PROCEDURE di_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich ">" zweier Datenobjekte "t_carry". <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:
  !!  - 2 Objekte des Typs "t_carry"
  !!  - 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"
  !!  - 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(>)
     MODULE PROCEDURE gt_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich ">=" zweier Datenobjekte "t_carry". <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:
  !!  - 2 Objekte des Typs "t_carry"
  !!  - 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"
  !!  - 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(>=)
     MODULE PROCEDURE ge_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich "<" zweier Datenobjekte "t_carry". <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:
  !!  - 2 Objekte des Typs "t_carry"
  !!  - 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"
  !!  - 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(<) 
     MODULE PROCEDURE lt_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Vergleich "<=" zweier Datenobjekte "t_carry". <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:
  !!  - 2 Objekte des Typs "t_carry"
  !!  - 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"
  !!  - 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(<=)
     MODULE PROCEDURE le_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_carry" auf Ungleichheit. <BR>
  !! Folgende Kombinationen der Operanden k&ouml;nnen verglichen werden:
  !!  - 2 Objekte des Typs "t_carry"
  !!  - 1 Feld von Objekten des Typs "t_carry" mit einem Objekt des Typs "t_carry"
  !!  - 2 Felder von Objekten des Typs "t_carry" 
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_carry_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_carry_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_carry_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_carry_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  !! Dividiert Datenobjekt 1 durch Datenobjekt 2 und gibt einen 
  !! INTEGER (KIND=long) - Wert zur&uuml;ck.<BR>
  !! Anwendungsfall: Berechne die Anzahl Kaugummis, die Du kaufen kannst,
  !! wenn Du (10 EURO, 23 CENT) hast, und 1 Kaugummi 
  !! (0 EURO, 4 CENT) kostet.<BR>
  !! Derzeit nur als Skalar-Version realisiert!
  INTERFACE OPERATOR(.DIV.)
     MODULE PROCEDURE idi_carry_0_0 ! Skalar .DIV. Skalar
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_carry                 ! Initialisieren (Modul)
  PUBLIC :: clear_carry                ! De-Initialisieren (Modul)
  PUBLIC :: setup_carry_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_carry_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_carry                  ! Erzeugen 
  PUBLIC :: kill_carry                 ! Vernichten
  PUBLIC :: ok_carry                   ! Pruefen
  PUBLIC :: print_carry                ! Drucken
  PUBLIC :: print_carry_static         ! Drucken aller statischen Daten
  PUBLIC :: print_carry_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_carry                  ! Setzen beider Komponenten gleichzeitig
  PUBLIC :: set_carry_values           ! Setzen der Komponente values
  PUBLIC :: set_carry_limits           ! Setzen der Komponente limits
  PUBLIC :: get_carry_values           ! Holen der Komponente values
  PUBLIC :: get_carry_limits           ! Holen der Komponente limits
  PUBLIC :: get_carry_sign             ! Hole das Vorzeichen eines b_carry-Objektes
  PUBLIC :: OPERATOR(==)               ! Operator "=="
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
!  PUBLIC :: OPERATOR(+)                ! Operator "+"
  PUBLIC :: add_carry                  ! Subroutine fuer "+"
!  PUBLIC :: OPERATOR(-)                ! Operator "-"
  PUBLIC :: sub_carry                  ! Subroutine fuer "-"
!  PUBLIC :: OPERATOR(*)                ! Operator "*"
  PUBLIC :: mul_carry                  ! Subroutine fuer "*" 
!  PUBLIC :: OPERATOR (/)               ! Operator "/"
  PUBLIC :: div_carry                  ! Subroutine fuer "/" 
  PUBLIC :: OPERATOR(>)                ! Operator ">"
  PUBLIC :: OPERATOR(>=)               ! Operator ">="
  PUBLIC :: OPERATOR(<)                ! Operator "<"
  PUBLIC :: OPERATOR(<=)               ! Operator "<="
  PUBLIC :: OPERATOR(/=)               ! Operator "/="
  PUBLIC :: OPERATOR(.DIV.)            ! Operator ".DIV."
  !
  ! [C.7.3] modulspezifische oeffentliche Methoden
  !
!  PUBLIC :: round_carry          ! Runde den Wert eines Carry-Objektes
  PUBLIC :: rnd_carry            ! Rundungsfunktion als Unterprogramm
  PUBLIC :: carry_to_real        ! Konvertiere Carry-Objekt in Real(double)
!  PUBLIC :: real_to_carry        ! Konvertiere Real(double) in Carry-Objekt
  PUBLIC :: double_to_carry      ! Konversionsfunktion als Unterprogramm
  PUBLIC :: abs_carry            ! Errechnet den Absolutwert des Carry-Objektes
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=07), PARAMETER :: c_modname      = 'b_carry' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.   ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1        ! 
  !! Anzahl der Datenkomponenten des Typs t_carry
  INTEGER           , PARAMETER :: c_nofcomp      =  2        ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)     ! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , SAVE :: trc_lun     = c_lun    ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0        ! 
  !
  ! [D.4] Schnittstellen
  !
  ! [D.5] Assignments
  !
  ! [D.6] Operatoren
  !
! Bitte fuer Windows NT diese Zeilen ergaenzen:
!    (entfernen Sie "!>WIN-NT:" z.B. mit sed)
!>WIN-NT:  PUBLIC:: eq_carry_0_0, eq_carry_0_1, eq_carry_1_0, eq_carry_1_1
!>WIN-NT:  PUBLIC:: ad_carry_0_0, ad_carry_0_1, ad_carry_1_0, ad_carry_1_1
!>WIN-NT:  PUBLIC:: su_carry_0_0, su_carry_0_1, su_carry_1_0, su_carry_1_1
!>WIN-NT:  PUBLIC:: mu_carry_I0_0, mu_carry_0_I0, mu_carry_I0_1, mu_carry_1_I0
!>WIN-NT:  PUBLIC:: mu_carry_D0_0, mu_carry_0_D0, mu_carry_D0_1, mu_carry_1_D0
     ! vielleicht spaeter noch realisieren :
     !  mu_carry_R0_0  ! Real   / Skalar
     !  mu_carry_0_R0  ! Skalar / Real
     !  mu_carry_R0_1  ! Real   / Vektor
     !  mu_carry_1_R0  ! Vektor / Real
!>WIN-NT:  PUBLIC:: di_carry_0_I0, di_carry_1_I0
     ! vielleicht spaeter noch realisieren :
     !  di_carry_0_D0  ! Skalar / Double
     !  di_carry_1_D0  ! Vektor / Double
     !  di_carry_0_R0  ! Skalar / Real
     !  di_carry_1_R0  ! Vektor / Real
     !  di_carry_0_0  ! Skalar / Skalar - Res: INTEGER
     !  di_carry_1_0  ! Vektor / Skalar
     !  di_carry_1_1  ! Vektor / Vektor
!>WIN-NT:  PUBLIC:: gt_carry_0_0, gt_carry_0_1, gt_carry_1_0, gt_carry_1_1
!>WIN-NT:  PUBLIC:: ge_carry_0_0, ge_carry_0_1, ge_carry_1_0, ge_carry_1_1
!>WIN-NT:  PUBLIC:: lt_carry_0_0, lt_carry_0_1, lt_carry_1_0, lt_carry_1_1
!>WIN-NT:  PUBLIC:: le_carry_0_0, le_carry_0_1, le_carry_1_0, le_carry_1_1
!>WIN-NT:  PUBLIC:: ne_carry_0_0, ne_carry_0_1, ne_carry_1_0, ne_carry_1_1
!>WIN-NT:  PUBLIC:: idi_carry_0_0

CONTAINS
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III   CCCCC            +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC  CC           +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP       UUUUUU   BBB  BB  LLL    III  CCC  CC           +
  !+           PPP        UUUU    BBBBBB   LLLLLL III   CCCCC            +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Globale Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_carry_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='init_carry_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_carry" version 1.21 of 03/23/07                    '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_carry_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.5] ggf. weitere Initialsierungsmethoden rufen
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_carry_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_carry_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='clear_carry_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_carry_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       ! ... derzeit nicht erforderlich
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_carry_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_carry_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_carry_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
    END IF
    !
  END SUBROUTINE setup_carry_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_carry_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_carry_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
    END IF
    !
  END SUBROUTINE setup_carry_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE new_carry_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='new_carry_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       NULLIFY ( this%values )
       NULLIFY ( this%limits )
    END IF
    !
  END SUBROUTINE new_carry_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_carry_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='new_carry_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_carry_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_carry_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_carry_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='kill_carry_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       ! de-allokieren dynamisch allokierbarer Feld-Komponenten
       IF ( no_error( ) ) CALL dealloc_carry_values( this )
       IF ( no_error( ) ) NULLIFY (this%limits)
       IF ( no_error( ) ) CALL new_carry_0 ( this )
    END IF
    !
  END SUBROUTINE kill_carry_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_carry_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='kill_carry_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_carry_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_carry_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_carry_0 ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_carry_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    LOGICAL :: l_ok3 ! 
    !
    l_ok   = .false.
    l_ok3  = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       l_ok(1) = ok_carry_values( this )
       l_ok(2) = ok_carry_limits( this )
       !
!       l_ok3 = SIZE(this%values) == SIZE(this%limits)
       !
!       IF ( .NOT. l_ok3 ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       !
    END IF
    !
    ok  = ALL( l_ok ) !.AND. l_ok3
    !
  END FUNCTION ok_carry_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_carry_1 ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_carry_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_carry_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_carry_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='print_carry_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_carry_values( this )
       IF ( no_error( ) ) CALL print_carry_limits( this )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     & 
                 IOSTAT  = stat )
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT('# Beginn Objekt t_carry ------------------------------')
8001 FORMAT('# Ende   Objekt t_carry ------------------------------')
    !
  END SUBROUTINE print_carry_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='print_carry_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_carry_0 ( this(i) )
          !
       END DO
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_carry_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_static_d ( )
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='print_carry_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_carry_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_carry         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_carry_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_carry_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       CALL print_error( all_errors(:) )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
  END SUBROUTINE print_carry_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! Setter f&uuml;r ein Objekt vom Typ "t_carry".<BR>
  !! weise der dynamischen Komponente "values" ein Feld zu <BR>
  !! und weise der dynamischen Komponente "limits" ein TARGET-Feld zu 
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_carry_0_1_IT1 ( this, val1, val2 )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "values"
    INTEGER (kind=long) , INTENT(IN)  :: val1(:) ! 
    !! Wert f&uuml;r Komponente "limits"
    INTEGER, TARGET, DIMENSION(:), INTENT(IN)  :: val2  ! 
    !
    CALL set_carry_values_0_1 ( this, val1 )
    CALL set_carry_limits_0_IT1 ( this, val2 )
    !
  END SUBROUTINE set_carry_0_1_IT1
  !
  !! Setter f&uuml;r Felder des Objektes vom Typ "t_carry".<BR>
  !! weise der dynamischen Komponente "values" ein Feld zu  <BR>
  !! und weise der dynamischen Komponente "limits" ein TARGET-Feld zu 
  SUBROUTINE set_carry_1_1_IT1 ( this, val1, val2 )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(INOUT) :: this(:)   ! 
    !! Werte f&uuml;r Komponente "values"
    INTEGER (kind=long) , INTENT(IN)  :: val1(:) ! 
    !! Wert f&uuml;r Komponente "limits"
    INTEGER, TARGET, DIMENSION(:), INTENT(IN)  :: val2  ! 
    !
    INTEGER :: i, n
    !
    n = size(val1)
    !
    DO i = 1, size(this)
       CALL set_carry_0_1_IT1 ( this(i), val1(1:n), val2(1:n) )
    ENDDO
    !
  END SUBROUTINE set_carry_1_1_IT1
  !
  !! weise der dynamischen Komponente "values" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_carry_values_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "values"
    INTEGER (kind=long) , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='set_carry_values_0_1'
    !! Vorzeichen
    INTEGER  :: ivz
    LOGICAL  :: l_sameSize
    !
    l_sameSize = .false.
    !
    IF ( ASSOCIATED(this%values) ) THEN
       !
       l_sameSize = ( SIZE(this%values,DIM=1) == SIZE(val,DIM=1) )
       !
    END IF
    !
    IF ( l_sameSize ) THEN
       !
       this%values(1:SIZE(val,DIM=1)) = val(1:SIZE(val,DIM=1))
       !
    ELSE
       !
       IF ( ok_initialised ( c_upname ) ) THEN
          !
          IF ( no_error( ) ) CALL dealloc_carry_values ( this            )
          IF ( no_error( ) ) CALL alloc_carry_values   ( this, SIZE(val) )
          IF ( no_error( ) ) this%values(1:SIZE(val)) = val(1:SIZE(val))
          !
       END IF
       !
    END IF
    !
    IF ( no_error( ) ) THEN
       !
       IF ( ASSOCIATED(this%limits) ) THEN
          !
          IF ( SIZE(this%values,DIM=1) == SIZE(this%limits,DIM=1) ) THEN
             !
             ivz = ivorzeichen( SIZE(val), this%values)
             !
             CALL bound_to_limits( ivz, this )
             !
          END IF
          !
       END IF
       !
    END IF
    !
  END SUBROUTINE set_carry_values_0_1
  !
  !! weise der dynamischen Komponente "values" ein Feld zu (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_carry_values_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "values"
    INTEGER (kind=long) , INTENT(IN)  :: val(:)  ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       IF ( any_error( ) ) EXIT
       CALL set_carry_values_0_1 ( this(i), val(:) )
    END DO
    !
  END SUBROUTINE set_carry_values_1_1
  !
  !! weise der Komponente "limits" ein Targetfeld mit den aktuellen limits zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_carry_limits_0_IT1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "limits"
    INTEGER, TARGET, DIMENSION(:), INTENT(IN)  :: val  ! 
    !! Vorzeichen
    INTEGER  :: ivz
    !
    this%limits => val
    !
    IF ( ASSOCIATED( this%values ) ) THEN
       !
       IF ( SIZE(this%limits, DIM=1) == SIZE(this%values, DIM=1) ) THEN
          !
          ivz = ivorzeichen( SIZE(this%values), this%values)
          !
          CALL bound_to_limits( ivz, this )
          !
       END IF
       !
    END IF
    !
  END SUBROUTINE set_carry_limits_0_IT1
  !
  !! weise der Komponente "limits" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_carry_limits_1_IT1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_carry) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "limits"
    INTEGER , TARGET , DIMENSION(:), INTENT(IN)  :: val     ! 
    !! Z&auml;hler
    INTEGER  :: i
    !
    DO i=1, SIZE(this)
       CALL set_carry_limits_0_IT1 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_carry_limits_1_IT1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die dynamische Feld Komponente "values" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_carry_values_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "values" (Vektor)
    INTEGER (kind=long) :: val(SIZE(this%values, DIM=1)) ! 
    !
    val = this%values
    !
  END FUNCTION get_carry_values_0_1
  !
  !! hole die dynamische Feld-Komponente "limits" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_carry_limits_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "limits" (Vektor)
    INTEGER :: val(SIZE(this%values)) ! 
    !
    val = this%limits(1:SIZE(this%values))
    !
  END FUNCTION get_carry_limits_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_carry_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( vgl_komponenten(this1, this2) == 0) ok = .true.
    !
  END FUNCTION eq_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_carry_1_0 ( this1,  this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_carry_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  !! Addiere zwei Datenobjekte und gib das Ergebnis in der Ergebnisvariablen zur&uuml;ck     <BR>
  !! vermeidet das immer w&auml;hrende Allokieren von Memory <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE add_carry_0_0_0 ( this1, this2, this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN)    :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN)    :: this2 ! 
    !! Ergebnis (muss beim Eingang ordentlich initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT) :: this  ! 
    ! Hilfsvariablen
    INTEGER :: ivz ! 
    !
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values + this2%values )
    !
    ivz = ivorzeichen( SIZE(this%values), this%values)
    !
    CALL bound_to_limits( ivz, this )
    !
  END SUBROUTINE add_carry_0_0_0
  !
  !! Addiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_carry_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Additionsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    ! Hilfsvariablen
    INTEGER         :: ivz
    !
    CALL new_carry ( this )
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values + this2%values )
    !
    ivz = ivorzeichen( SIZE(this%values), this%values)
    !
    CALL bound_to_limits( ivz, this )
    !
  END FUNCTION ad_carry_0_0
  !
  !! Addiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_carry_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1,DIM=1)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this1,DIM=1)
       this(i) = ad_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ad_carry_1_0
  !
  !! Addiere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_carry_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Additionsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this2)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       this(i) = ad_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ad_carry_0_1
  !
  !! Addiere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ad_carry_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    ! Rueckgabewert
    !! Additionsergebnis (Vektor)
    TYPE (t_carry) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariablen
    INTEGER :: i !
    !
    DO i=1, MIN(SIZE(this1),SIZE(this2))
       this(i) = ad_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ad_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  !! Subtrahiere zwei Datenobjekte und gib das Ergebnis in der Ergebnisvariablen zur&uuml;ck <BR>
  !! vermeidet das immer w&auml;hrende Allokieren von Memory <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE sub_carry_0_0_0 ( this1, this2, this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN)    :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN)    :: this2 ! 
    !! Ergebnis (muss beim Eingang ordentlich initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT) :: this  ! 
    ! Hilfsvariablen
    INTEGER :: ivz ! 
    !
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values - this2%values )
    !
    ivz = ivorzeichen( size(this%values), this%values)
    !
    call bound_to_limits( ivz, this )
    !
  END SUBROUTINE sub_carry_0_0_0
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_carry_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Subtraktionsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    !! Vorzeichen
    INTEGER :: ivz ! 
    !
    CALL new_carry ( this )
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values - this2%values )
    !
    ivz = ivorzeichen( size(this%values), this%values)
    !
    call bound_to_limits( ivz, this )
    !
  END FUNCTION su_carry_0_0
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_carry_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION su_carry_1_0
  !
  !! Subtrahiere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_carry_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION su_carry_0_1
  !
  !! Subtrahiere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION su_carry_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Subtraktionsergebnis (Vektor)
    TYPE (t_carry) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = su_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION su_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  !! Multiplikation einer Integer-Groesse mit einem Skalar <BR>
  !! vermeidet das immer w&auml;hrende Allokieren von Memory <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE mul_carry_0_0_in ( imul, this1, this ) 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: imul  ! 
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Ergebnis (muss beim Eingang ordentlich initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT)   :: this  ! 
    ! Hilfsvariablen
    INTEGER   :: ivz, i ! 
    !! maximal zulaessigen Multiplikations-Faktor
    INTEGER (kind=long) :: max_mul   ! 
    !! Rest-Faktor bei portionierter Multiplikation
    INTEGER (kind=long) :: rest_mul  ! 
    !! Hilfsobjekt fuer Produkt-Portion
    TYPE (t_carry)      :: pro     ! 
    !! Hilfsobjekt zum Aufsummieren
    TYPE (t_carry)      :: sum     ! 
    !
    ! Berechnung des maximal zulaessigen Multiplikations-Faktors :
    ! Zehntel der groesstmoeglichen Int(Long)-Zahl durch das groesste vorhandene
    ! Limit, wobei limits(1) natuerlich nicht beruecksichtigt wird.
    max_mul = huge(this1%values(1)) / 10
    max_mul = max_mul / maxval(this1%limits(2:))
    !
    CALL set_carry_limits ( this, this1%limits )
    !
    IF ( imul > max_mul ) THEN
       !
       CALL new_carry( pro )
       CALL set_carry_limits ( pro, this1%limits )
       CALL new_carry( sum )
       CALL set_carry_limits ( sum, this1%limits )
       !
       CALL set_carry_values ( this, 0*this1%values )
       DO i = 1, imul / max_mul
          !
          CALL set_carry_values ( pro , this1%values*max_mul )
          CALL set_carry_values ( sum , this%values + pro%values )
          CALL set_carry_values ( this, sum%values )
          !
       END DO
       !
       rest_mul = imul - ((imul / max_mul)*max_mul)
       !
       CALL set_carry_values ( pro , this1%values*rest_mul )
       CALL set_carry_values ( sum , this%values + pro%values )
       CALL set_carry_values ( this, sum%values )
       !
       CALL kill_carry ( pro )
       CALL kill_carry ( sum )
       !
    ELSE
       CALL set_carry_values ( this, this1%values*imul )
    END IF
    !
    ivz = ivorzeichen( size(this1%values,DIM=1), this%values)
    !
    CALL bound_to_limits( ivz, this )
    !
  END SUBROUTINE mul_carry_0_0_in
  !
  !! Multipliziere Integer-Wert mit Datenobjekt ( Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_I0_0 ( imul, this1 ) &
         RESULT( this )
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: imul ! 
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    ! Hilfsvariablen
    INTEGER   :: ivz, i
    !! maximal zulaessigen Multiplikations-Faktor
    INTEGER (kind=long) :: max_mul   ! 
    !! Rest-Faktor bei portionierter Multiplikation
    INTEGER (kind=long) :: rest_mul  ! 
    !! Hilfsobjekt fuer Produkt-Portion
    TYPE (t_carry)      :: pro     ! 
    !! Hilfsobjekt zum Aufsummieren
    TYPE (t_carry)      :: sum     ! 
    !
    ! Berechnung des maximal zulaessigen Multiplikations-Faktors :
    ! Zehntel der groesstmoeglichen Int(Long)-Zahl durch das groesste vorhandene
    ! Limit, wobei limits(1) natuerlich nicht beruecksichtigt wird.
    max_mul = huge(this1%values(1)) / 10
    max_mul = max_mul / maxval(this1%limits(2:))
    !
    CALL new_carry( this )
    CALL set_carry_limits ( this, this1%limits )
    !
    IF ( imul > max_mul ) THEN
       !
       CALL new_carry( pro )
       CALL set_carry_limits ( pro, this1%limits )
       CALL new_carry( sum )
       CALL set_carry_limits ( sum, this1%limits )
       !
       CALL set_carry_values ( this, 0*this1%values )
       DO i = 1, imul / max_mul
          !
          CALL set_carry_values ( pro , this1%values*max_mul )
          CALL set_carry_values ( sum , this%values + pro%values )
          CALL set_carry_values ( this, sum%values )
          !
       END DO
       !
       rest_mul = imul - ((imul / max_mul)*max_mul)
       !
       CALL set_carry_values ( pro , this1%values*rest_mul )
       CALL set_carry_values ( sum , this%values + pro%values )
       CALL set_carry_values ( this, sum%values )
       !
       CALL kill_carry ( pro )
       CALL kill_carry ( sum )
       !
    ELSE
       CALL set_carry_values ( this, this1%values*imul )
    END IF
    !
    ivz = ivorzeichen( size(this1%values,DIM=1), this%values)
    !
    CALL bound_to_limits( ivz, this )
    !
  END FUNCTION mu_carry_I0_0
  !
  !! Multipliziere Datenobjekt ( Skalar ) mit Integer-Wert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_0_I0 ( this1, imul ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: imul ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    !
    this = mu_carry_I0_0( imul, this1 )
    !
  END FUNCTION mu_carry_0_I0
  !
  !! Multipliziere Integer-Wert mit Datenobjekten ( Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_I0_1 ( imul, this1 ) &
         RESULT( this )
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: imul ! 
    !! Objekt (Vektor)
    TYPE (t_carry) , INTENT(IN)      :: this1(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_carry_I0_0( imul, this1(i) )
    END DO
    !
  END FUNCTION mu_carry_I0_1
  !
  !! Multipliziere Datenobjekten ( Vektor ) mit Integer-Wert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_1_I0 ( this1, imul ) &
         RESULT( this )
    !! Objekt (Vektor)
    TYPE (t_carry)      , INTENT(IN) :: this1(:) ! 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: imul ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_carry_I0_0( imul, this1(i) )
    END DO
    !
  END FUNCTION mu_carry_1_I0
  !
  !! Multiplikation einer Double-Groesse mit einem Skalar <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE mul_carry_0_0_dp ( dmul, this1, this )
    !! Real-Wert
    REAL (kind=double) , INTENT(IN)  :: dmul  ! 
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Ergebnis (muss beim Eingang ordentlich initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT)   :: this  ! 
    ! Hilfsvariablen
    REAL (kind=double)  :: residuum
    REAL (kind=double)  :: dfeld(SIZE(this1%values))
    INTEGER   :: i
    INTEGER   :: ivz
    !
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values )
    !
    dfeld = this%values
    !
    dfeld = dmul * dfeld
    !
    ivz = rvorzeichen( SIZE(this%values), dfeld )
    !
    CALL r_bound_to_limits( SIZE(this%values), ivz, dfeld, this%limits)
    !
    residuum = 0.0
    DO i= 1, SIZE(this%values)-1
       !
       this%values(i) = INT(dfeld(i) + residuum)
       residuum = ( dfeld(i) + residuum - DBLE(this%values(i)) ) * DBLE(this%limits(i+1))
       ! 
    END DO
    !
    this%values( SIZE(this%values) ) = INT( dfeld(SIZE(this%values)) + residuum )
    !
    CALL bound_to_limits( ivz, this )
    !
  END SUBROUTINE mul_carry_0_0_dp
  !
  !! Multipliziere Real(Kind=Double)-Wert mit Datenobjekt ( Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_D0_0 ( dmul, this1 ) &
         RESULT( this )
    !! Real-Wert
    REAL (kind=double) , INTENT(IN)  :: dmul ! 
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    !
    REAL (kind=double)  :: residuum
    REAL (kind=double)  :: dfeld(SIZE(this1%values))
    INTEGER   :: i
    INTEGER   :: ivz
    !
    CALL new_carry ( this )
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values )
    !
    dfeld = this%values
    !
    dfeld = dmul * dfeld
    !
    ivz = rvorzeichen( SIZE(this%values), dfeld )
    !
    CALL r_bound_to_limits( SIZE(this%values), ivz, dfeld, this%limits)
    !
    residuum = 0.0
    DO i= 1, SIZE(this%values)-1
       !
       this%values(i) = INT(dfeld(i) + residuum)
       residuum = ( dfeld(i) + residuum - DBLE(this%values(i)) ) * DBLE(this%limits(i+1))
       ! 
    END DO
    !
    this%values( SIZE(this%values) ) = INT( dfeld(SIZE(this%values)) + residuum )
    !
    CALL bound_to_limits( ivz, this )
    !
  END FUNCTION mu_carry_D0_0
  !
  !! Multipliziere Datenobjekt ( Skalar ) mit Real(Kind=Double)-Wert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_carry_0_D0 ( this1, dmul ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Real-Wert
    REAL (kind=double) , INTENT(IN) :: dmul ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    !
    this = mu_carry_D0_0( dmul, this1 )
    !
  END FUNCTION mu_carry_0_D0
  !
  !! Multipliziere Real(Kind=Double)-Wert mit Datenobjekten ( Vektor ) <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION mu_carry_D0_1 ( dmul, this1 ) &
         RESULT( this )
    !! Real-Wert
    REAL (kind=double) , INTENT(IN)  :: dmul ! 
    !! Objekt (Vektor)
    TYPE (t_carry)     , INTENT(IN)  :: this1(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_carry_D0_0( dmul, this1(i) )
    END DO
    !
  END FUNCTION mu_carry_D0_1
  !
  !! Multipliziere Datenobjekten ( Vektor ) mit Real(Kind=Double)-Wert <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION mu_carry_1_D0 ( this1, dmul ) &
         RESULT( this )
    !! Objekt (Vektor)
    TYPE (t_carry)      , INTENT(IN) :: this1(:) ! 
    !! Real-Wert
    REAL (kind=double)  , INTENT(IN) :: dmul ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_carry_D0_0( dmul, this1(i) )
    END DO
    !
  END FUNCTION mu_carry_1_D0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  !! Division eines Datenobjekts ( Skalar ) durch ein 
  !! anderes Datenobjekt  ( Skalar ).<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION idi_carry_0_0 ( this1, this2 ) &
         RESULT( nintervals )
    ! Formalparameter
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 !
    !! Objekt2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 !
    !! Divisionsergebnis (Skalar)
    INTEGER (kind=long) :: nintervals !
    !! Hilfsobjekt vom Typ "t_carry"
    TYPE (t_carry) :: hlpcarry1, hlpcarry2, hlpcarry3
    !
    IF ( ALL(this2%values == 0)) THEN
       nintervals = huge(nintervals); RETURN
    ENDIF
    !
    nintervals = 0
    hlpcarry1 = abs_carry(this1)
    hlpcarry2 = abs_carry(this2)
    !
    DO
       IF (hlpcarry1 < hlpcarry2) EXIT
       hlpcarry3 =  hlpcarry1 - hlpcarry2
       hlpcarry1 = hlpcarry3
       nintervals = nintervals + 1
    ENDDO
    !
    nintervals = nintervals * get_carry_sign(this1)* &
    get_carry_sign(this2)
    !
  END FUNCTION idi_carry_0_0
  !
  !! Division eines Skalars mit einer Integer-Groesse <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE div_carry_0_0_in ( this1, idiv, this )
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN)      :: this1 ! 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: idiv  ! 
    !! Ergebnis (muss beim Eingang ordentlich initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT)   :: this  ! 
    ! Hilfsvariablen
    INTEGER  :: i
    INTEGER  :: newkomp
    INTEGER (kind=long) :: residuum
    INTEGER (kind=long) :: oldkomp
    !
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values )
    !    
    residuum = 0
    !
    DO i=1, SIZE(this%values)
       !
       oldkomp = this%values(i) + residuum
       !
       newkomp = oldkomp/idiv
       IF ( i < SIZE(this%values) ) residuum = (oldkomp - newkomp*idiv) * this%limits(i+1)
       !
       this%values(i) = newkomp
       !
    END DO
    !
  END SUBROUTINE div_carry_0_0_in
  !
  !! Division eines Datenobjekts ( Skalar ) durch eine
  !! INTEGER(long)  ( Skalar ).<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_carry_0_I0 ( this1, idiv ) &
         RESULT( this )
    !! Objekt (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: idiv ! 
    !! Divisionsergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    ! Hilfsvariablen
    INTEGER  :: i
    INTEGER  :: newkomp
    INTEGER (kind=long) :: residuum
    INTEGER (kind=long) :: oldkomp
    !
    CALL new_carry (this)
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values )
    !    
    residuum = 0
    !
    DO i=1, SIZE(this%values)
       !
       oldkomp = this%values(i) + residuum
       !
       newkomp = oldkomp/idiv
       IF ( i < SIZE(this%values) ) residuum = (oldkomp - newkomp*idiv) * this%limits(i+1)
       !
       this%values(i) = newkomp
       !
    END DO
    !
  END FUNCTION di_carry_0_I0
  !
  !! Dividiere Datenobjekte ( Vektor ) durch eine Integer-Zahl <BR>
  !! mit Restuebertrag. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_carry_1_I0 &
       ( this1,  &
         idiv ) &
         RESULT( this )
    !
    ! Formalparameter
    !! Objekt (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Integer-Wert
    INTEGER (kind=long) , INTENT(IN) :: idiv ! 
    !
    ! Rueckgabewert
    !! Divisionsergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='di_carry_1_I0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_carry_0_I0( this1(i), idiv )
    END DO
    !
  END FUNCTION di_carry_1_I0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_carry_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( vgl_komponenten(this1, this2) > 0) ok = .true.
    !
  END FUNCTION gt_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_carry_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_carry_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = gt_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_carry_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( vgl_komponenten(this1, this2) >= 0) ok = .true.
    !
  END FUNCTION ge_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_carry_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_carry_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ge_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_carry_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( vgl_komponenten(this1, this2) < 0) ok = .true.
    !
  END FUNCTION lt_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_carry_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_carry_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = lt_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_carry_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( vgl_komponenten(this1, this2) <= 0) ok = .true.
    !
  END FUNCTION le_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_carry_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_carry_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_carry_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_carry_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = le_carry_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_carry_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_carry_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_carry_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_carry_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_carry_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_carry_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_carry_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_carry) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_carry_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-MODULSPEZIFISCHE-Methoden <<< [ERR_NO <0]
  ! ----------------------------------------------------------------------
  !
  !! Rundet den Wert eines Carry-Objektes (Skalar) <BR>
  !! vermeidet das immer w&auml;hrende Allokieren von Memory <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE rnd_carry_0_0_0 ( this1, bezugs_komponente, this )
    !! Datenobjekt (Skalar)
    TYPE (t_carry) , INTENT(IN)     :: this1 ! 
    !! Bezugs-Komponente
    INTEGER        , INTENT(INOUT)  :: bezugs_komponente ! 
    !! Ergebnis (muss beim Eingang korrekt initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT)  :: this   ! 
    !! Vorzeichen      
    INTEGER          :: ivz ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values ) 
    !
    IF ( bezugs_komponente < 1 .OR. bezugs_komponente > SIZE(this%values) ) &
      bezugs_komponente = SIZE(this%values)
    !
    ivz = ivorzeichen( SIZE(this%values), this%values )
    !
    CALL bound_to_limits( ivz, this )
    !
    IF ( bezugs_komponente == SIZE(this%values) ) RETURN
    !
    IF ( ivz * this%values(bezugs_komponente+1) >= (this%limits(bezugs_komponente+1)/2) ) then
       this%values(bezugs_komponente) = this%values(bezugs_komponente) + ivz
    END IF
    !
    DO i = bezugs_komponente+1, SIZE(this%values)
       this%values(i) = 0
    END DO
    !
    CALL bound_to_limits( ivz, this )
    !
  END SUBROUTINE rnd_carry_0_0_0
  !
  !! Rundet den Wert eines Carry-Objektes (Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_carry_0 ( this1, bezugs_komponente) &
       RESULT( this )
    !! Datenobjekt (Skalar)
    TYPE (t_carry), INTENT(IN)      :: this1 ! 
    !! Bezugs-Komponente
    INTEGER       , INTENT(INOUT)   :: bezugs_komponente !
    !! Rundungssergebnis (Skalar)
    TYPE (t_carry) :: this ! 
    !! Vorzeichen      
    INTEGER          :: ivz ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    CALL new_carry ( this )
    CALL set_carry_limits ( this, this1%limits )
    CALL set_carry_values ( this, this1%values ) 
    !
    IF ( bezugs_komponente < 1 .OR. bezugs_komponente > SIZE(this%values) ) &
      bezugs_komponente = SIZE(this%values)
    !
    ivz = ivorzeichen( SIZE(this%values), this%values )
    !
    CALL bound_to_limits( ivz, this )
    !
    IF ( bezugs_komponente == SIZE(this%values) ) RETURN
    !
    IF ( ivz * this%values(bezugs_komponente+1) >= (this%limits(bezugs_komponente+1)/2) ) then
       this%values(bezugs_komponente) = this%values(bezugs_komponente) + ivz
    END IF
    !
    DO i = bezugs_komponente+1, SIZE(this%values)
       this%values(i) = 0
    END DO
    !
    CALL bound_to_limits( ivz, this )
    !
  END FUNCTION round_carry_0
  !
  !! Rundet die Werte von Feldern von Carry-Objekten (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION round_carry_1 ( this1, bezugs_komponente) &
       RESULT( this )
    !! Datenobjekt (Vektor)
    TYPE (t_carry), INTENT(IN)      :: this1(:) ! 
    !! Bezugs-Komponente
    INTEGER       , INTENT(INOUT)   :: bezugs_komponente !
    !! Rundungssergebnis (Vektor)
    TYPE (t_carry) :: this(SIZE(this1)) ! 
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       this(i) = round_carry_0( this1(i), bezugs_komponente )
    END DO
    !
  END FUNCTION round_carry_1
  !
  !! Ermittelt den Real-Wert eines Carry-Objektes (Skalar) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION carry_to_real_0 ( this, bezugs_komponente) &
         RESULT( rwert )
    !! Datenobjekt (Skalar)
    TYPE (t_carry)    , INTENT(IN)     :: this ! 
    !! Bezugs-Komponente
    INTEGER           , INTENT(INOUT)  :: bezugs_komponente !
    !! Real-Wert des Carry-Objektes
    REAL (KIND=double)    :: rwert !
    !! Vorzeichen      
    INTEGER            :: ivz ! 
    !! Z&auml;hler      
    INTEGER             :: i   ! 
    !! 
    REAL (KIND=Double ) :: rdiv ! 
    !
    IF (bezugs_komponente < 1 .OR. bezugs_komponente > SIZE(this%values)) &
         bezugs_komponente = 1
    !
    ivz = ivorzeichen( SIZE(this%values), this%values )
    !
    rdiv = 1.0_Double
    IF (bezugs_komponente > 1) THEN
       DO i = 2, bezugs_komponente
          rdiv = rdiv * REAL(this%limits(i),KIND=Double) 
       END DO
    END IF
    !
    rdiv = 1.0_Double / rdiv
    rwert = REAL( ivz * this%values(1), KIND=Double ) / rdiv
    DO i = 2, SIZE(this%values)
       rdiv  = rdiv  * REAL(this%limits(i),KIND=Double)
       rwert = rwert + REAL(ivz*this%values(i),KIND=Double) / rdiv
    END DO
    rwert = rwert * ivz
    !
  END FUNCTION carry_to_real_0
  !
  !! Ermittelt die Real-Werte von Carry-Objekten (Vektor) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION carry_to_real_1 ( this, bezugs_komponente) &
         RESULT( rwert )
    !! Datenobjekt (Vektor)
    TYPE (t_carry)    , INTENT(IN)     :: this(:) ! 
    !! Bezugs-Komponente
    INTEGER           , INTENT(INOUT)  :: bezugs_komponente !
    !! Real-Werte der Carry-Objekte
    REAL (KIND=double)  :: rwert(SIZE(this)) !
    !! Z&auml;hler      
    INTEGER          :: i !
    !
    DO i=1,SIZE(this)
       rwert(i) = carry_to_real_0( this(i), bezugs_komponente )
    END DO
    !
  END FUNCTION carry_to_real_1
  !
  !! &Uuml;bertrage eine Real(double)-Zahl in ein Carry-Objekt (Skalar)<BR>
  !! vermeidet das immer w&auml;hrende Allokieren von Memory <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE double_to_carry_0_dp ( rwert, bezugs_komponente, ilimit, this )
    !! Real-Wert
    REAL (KIND=double), INTENT(IN)    :: rwert !
    !! Bezugs-Komponente
    INTEGER           , INTENT(INOUT) :: bezugs_komponente !
    !! Grenzen der Kategorien
    INTEGER, TARGET    , INTENT(IN)   :: ilimit(:) !
    !! Ergebnis (muss im Eingang korrekt initialisiert sein)
    TYPE (t_carry) , INTENT(INOUT)    :: this ! 
    !! Vorzeichen      
    INTEGER              :: ivz ! 
    !! Z&auml;hler      
    INTEGER              :: i !
    !! Hilfswert 
    REAL(KIND=double)    :: rhlp !
    !! Hilfsfeld
    INTEGER (kind=long)  :: ifeld(SIZE(ilimit))
    !
    IF ( bezugs_komponente < 1 .OR. bezugs_komponente > SIZE(ilimit) ) &
         bezugs_komponente = 1 
    !
    ivz = 1
    IF ( rwert < 0 ) ivz = -1
    !
    rhlp = rwert * ivz
    !
    IF (bezugs_komponente > 1) THEN
       !
       DO i = bezugs_komponente, 2, -1
          !
          rhlp = rhlp / REAL(ilimit(i))
          !
       END DO
       !
    END IF
    !
    DO i = 1, SIZE(ilimit)
       !
       IF (i > 1) rhlp = rhlp * REAL(ilimit(i))
       ifeld(i) = INT(rhlp)
       rhlp = rhlp - REAL(ifeld(i))
       !
    END DO
    !
    ifeld = ifeld * ivz
    !
    CALL set_carry_values( this, ifeld )
    CALL set_carry_limits( this, ilimit )
    !
  END SUBROUTINE double_to_carry_0_dp
  !
  !! &Uuml;bertrage eine Real(double)-Zahl in ein Carry-Objekt (Skalar)<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_to_carry_0 ( rwert, bezugs_komponente, ilimit) &
         RESULT( this )
    !! Real-Wert
    REAL (KIND=double), INTENT(IN)     :: rwert !
    !! Bezugs-Komponente
    INTEGER           , INTENT(INOUT)  :: bezugs_komponente !
    !! Grenzen der Kategorien
    INTEGER, TARGET    , INTENT(IN)     :: ilimit(:) !
    !! Datenobjekt (Skalar)
    TYPE (t_carry)    :: this ! 
    !! Vorzeichen      
    INTEGER              :: ivz ! 
    !! Z&auml;hler      
    INTEGER              :: i !
    !! Hilfswert 
    REAL(KIND=double)    :: rhlp !
    !! Hilfsfeld
    INTEGER (kind=long)  :: ifeld(SIZE(ilimit))
    !
    IF ( bezugs_komponente < 1 .OR. bezugs_komponente > SIZE(ilimit) ) &
         bezugs_komponente = 1 
    !
    ivz = 1
    IF ( rwert < 0 ) ivz = -1
    !
    rhlp = rwert * ivz
    !
    IF (bezugs_komponente > 1) THEN
       !
       DO i = bezugs_komponente, 2, -1
          !
          rhlp = rhlp / REAL(ilimit(i))
          !
       END DO
       !
    END IF
    !
    DO i = 1, SIZE(ilimit)
       !
       IF (i > 1) rhlp = rhlp * REAL(ilimit(i))
       ifeld(i) = INT(rhlp)
       rhlp = rhlp - REAL(ifeld(i))
       !
    END DO
    !
    ifeld = ifeld * ivz
    !
    CALL new_carry( this )
    CALL set_carry_values( this, ifeld )
    CALL set_carry_limits( this, ilimit )
    !
  END FUNCTION real_to_carry_0
  !
  !! &Uuml;bertrage Real(double)-Zahlen in Carry-Objekte (Vektor)<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION real_to_carry_1 ( rwert, bezugs_komponente, ilimit ) &
         RESULT( this )
    !! Real-Werte
    REAL (KIND=double), INTENT(IN)     :: rwert(:) !
    !! Bezugs-Komponente
    INTEGER           , INTENT(INOUT)  :: bezugs_komponente !
    !! Grenzen der Kategorien
    INTEGER           , INTENT(IN)     :: ilimit(:) !
    !! Datenobjekt (Vektor)
    TYPE (t_carry)    :: this(SIZE(rwert)) ! 
    !! Z&auml;hler      
    INTEGER              :: i !
    !
    DO i=1,SIZE(rwert)
       this(i) = real_to_carry_0( rwert(i), bezugs_komponente, ilimit )
    END DO
    !
  END FUNCTION real_to_carry_1
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+     PPPPPP   RRRRRR   III  VVV  VVV     AA    TTTTTTT  EEEEEEE      +
  !+     PPP  PP  RRR  RR  III  VVV  VVV    AAAA     TTT    EEE          +
  !+     PPP  PP  RRR  RR  III  VVV  VVV   AAA AA    TTT    EEE          +
  !+     PPPPPP   RRRRRR   III  VVV  VVV  AAA  AAA   TTT    EEEEEE       +
  !+     PPP      RRRRR    III  VVV  VVV  AAA  AAA   TTT    EEE          +
  !+     PPP      RRR RR   III   VVV VV   AAAAAAAA   TTT    EEE          +
  !+     PPP      RRR  RR  III    VVVV    AAA  AAA   TTT    EEE          +
  !+     PPP      RRR   RR III     VV     AAA  AAA   TTT    EEEEEEE      +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Lokale Methoden:
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname )         &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       !
       WRITE(*,*) ' *** Warnung *** Modul "b_carry" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_carry ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname )          &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_carry_all_errors ( )
    !! Z&auml;hlervariable
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       !
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist nicht initialisiert\n'//&
               '--> INIT_carry ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_carry ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_carry"\n'//&
               'Typ-Komponente = "values"\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_carry"\n'//&
               'Typ-Komponente = "limits"\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_carry"\n'//&
               'Typ-Komponente = "values"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_carry"\n'//&
               'Typ-Komponente = "limits"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_carry"\n'//&
               'Feldgroesse der Komponenten unterschiedlich\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_carry"\n'//&
               'Typ-Komponente = "values"\n'//&
               '--> Code in Modul "b_carry" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_carry"\n'//&
               'Typ-Komponente = "limits"\n'//&
               '--> Code in Modul "b_carry" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_carry"\n'//&
               '--> Code in Modul "b_carry" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_carry"\n'//&
               'Typ-Komponente = "values"\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_carry"\n'//&
               'Typ-Komponente = "limits"\n'//&
               '--> Code in Modul "b_carry" pruefen' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_carry_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_carry_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_carry_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte ALLOC-Routinen bitte unbedingt loeschen <--------
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "values" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_carry_values ( this, idim )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "values"
    INTEGER               , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_carry_values' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF (associated(this%values)) DEALLOCATE(this%values)
    ALLOCATE ( this%values(idim), STAT=stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8010, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_carry_values
  !
  !! Allokieren der dynamischen Feld-Komponente "limits" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_carry_limits ( this, idim )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "limits"
    INTEGER               , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_carry_limits' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%limits(idim), STAT=stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8020, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_carry_limits
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte DEALLOC-Routinen bitte unbedingt loeschen <------
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "values" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_carry_values ( this )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_carry_values' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%values ) ) THEN
       DEALLOCATE ( this%values, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
       NULLIFY ( this%values ) 
    END IF
    !
  END SUBROUTINE dealloc_carry_values
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "values" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_carry_values ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_carry_values' ! 
    !
    ok = ASSOCIATED(this%values)
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_carry_values
  !
  !! Pr&uuml;fe, ob die Komponente "limits" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_carry_limits ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_carry_limits' ! 
    INTEGER :: i
    !
    ok = associated(this%limits)
    !
    DO i = 2, SIZE(this%values)

      ok = ok .and. (this%limits(i) > 0)

    ENDDO
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_carry_limits
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "values" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_values ( this )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_carry_values' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE( UNIT    = prn_lun,  &
           FMT     = *,        &
           IOSTAT  = stat ) "THIS.VALUES=",this%values  ! schreibe Inhalt von this%values
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
  END SUBROUTINE print_carry_values
  !
  !! Drucke den Inhalt der Komponente "limits" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_carry_limits ( this )
    !! Datenobjekt
    TYPE (t_carry) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_carry_limits' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE( UNIT    = prn_lun,  &
           FMT     = *,        &
           IOSTAT  = stat ) "THIS.LIMITS=",this%limits  ! schreibe Inhalt von this%limits
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
  END SUBROUTINE print_carry_limits
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE modul-spezifische Methoden <<< [ERR_NO < 0 ]
  ! ----------------------------------------------------------------------
  !
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION rvorzeichen ( n, feld )
    !! Feldgroesse
    INTEGER           , INTENT(IN)  :: n
    !! Feld
    REAL (kind=double), INTENT(IN)  :: feld(:)
    !! Vorzeichen
    INTEGER :: rvorzeichen
    ! Lokale Parameter / Variablen
    INTEGER :: i, sign_ifeld
    !
    rvorzeichen =1
    IF ( n <= 0 ) RETURN
    !
    DO i = n, 1, -1
       sign_ifeld = 1
       if(feld(i) < 0.) sign_ifeld = -1
       if(feld(i) /= 0. ) rvorzeichen = sign_ifeld
    END DO
    !
  END FUNCTION rvorzeichen
  !
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_carry_sign_0(this) RESULT(isign)
     !! Objekt des Typs "t_carry".
     TYPE(t_carry), INTENT(IN) :: this
     !! R&uuml;ckgabwert mit dem Vorzeichen 
     INTEGER :: isign
     !
     isign = 0
     !
     IF (ASSOCIATED(this%values)) THEN
        isign = ivorzeichen(SIZE(this%values), this%values)
     ENDIF
     !
  END FUNCTION get_carry_sign_0
  !! 
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_carry_sign_1(this) RESULT(isign)
     !! Feld von Objekten des Typs "t_carry".
     TYPE(t_carry), INTENT(IN) :: this(:)
     !! R&uuml;ckgabefeld mit den Vorzeichen 
     INTEGER :: isign(SIZE(this))
     INTEGER :: i
     !
     DO i =1, SIZE(this)
        isign(i) = get_carry_sign_0(this(i))
     ENDDO
     !
  END FUNCTION get_carry_sign_1
  !! 
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ivorzeichen ( n, ifeld )
    ! Formalparameter
    INTEGER, INTENT(IN) :: n
    INTEGER (kind=long), INTENT(IN) :: ifeld(n)
    !! Vorzeichen
    INTEGER :: ivorzeichen
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Hilfsfeld
    REAL (kind=double)  :: feld(n)
    !
    DO i = 1, n
       feld(i) = DBLE(ifeld(i))
    END DO
    !
    ivorzeichen = rvorzeichen( n, feld )
    !
  END FUNCTION ivorzeichen
  !
  !! Korrektur der Komponenten innnerhalb der vorgeschriebenen
  !! Grenzen.<BR>
  !! (Version fuer Integer-Feld)
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE bound_to_limits ( ivz, this )
    !! Vorzeichen      
    INTEGER       , INTENT(IN)     :: ivz ! 
    !! Datenobjekt (Skalar)
    TYPE (t_carry)  :: this !
    !
    ! Lokale Parameter und Variablen
    !
    INTEGER             :: i
    INTEGER (kind=long) :: ianz
    INTEGER             :: sign_ifeld
    LOGICAL             :: ok
    INTEGER (kind=long) :: i_res, i_modulo
    !
    DO i = SIZE(this%values), 2, -1
       !
       IF (this%limits(i) <= 0) CYCLE
       !
       sign_ifeld = 1
       IF ( this%values(i) <  0 ) sign_ifeld = -1
       !
       ianz = ABS(this%values(i)) / this%limits(i)
       !
       ok = .false.
       !
       this%values(i)   = sign_ifeld * ( ABS(this%values(i)) - ianz * this%limits(i) )
       ! 
       this%values(i-1) = sign_ifeld * ianz + this%values(i-1)
       !
       IF ( this%values(i) * ivz < 0 ) THEN
          ! 
          this%values(i-1)  = this%values(i-1) - ivz
          this%values(i)    = this%values(i) + this%limits(i)  * ivz
          !
       END IF
       !
    ENDDO
    !
    IF ( this%limits(1) <= 0 ) RETURN
    !
    i_res = ABS(this%values(1)); i_modulo = this%limits(1)
    IF ( i_res > this%limits(1)-1) then
       !
       i_res = MOD( i_res, i_modulo )
       !
    END IF
    !
  END SUBROUTINE bound_to_limits
  !
  !! Korrektur der Komponenten innnerhalb der vorgeschriebenen
  !! Grenzen.<BR>
  !! (Version fuer Real-Feld)
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE r_bound_to_limits (n, ivz, feld, ilimit)
    ! Formalparameter
    !! 
    INTEGER, INTENT(in) :: n, ivz,  ilimit(n)
    REAL (kind=double)  :: feld(n)
    !
    ! Lokale Parameter und Variablen
    !
    INTEGER              :: i 
    INTEGER (kind=long)  :: ianz
    REAL (kind=double)   :: sign_ifeld
    LOGICAL              :: ok
    REAL (kind=double)   :: d_res
    !
    DO i = n, 2, -1
       !
       IF ( ilimit(i) <= 0 ) CYCLE 
       sign_ifeld = 1.0D0
       IF ( feld(i) <  0.0D0 ) sign_ifeld = -1.0D0
       ianz = ABS(feld(i)) / DBLE(ilimit(i))
       !
       ok = .false.
       !
       feld(i)   = sign_ifeld * (ABS(feld(i)) - DBLE(ianz*ilimit(i)))
       feld(i-1) = sign_ifeld * DBLE(ianz) + feld(i-1)
       !
       IF ( (feld(i)*dble(ivz) < 0.0D0 ) ) THEN
          ! 
          feld(i-1) = feld(i-1) - dble(ivz)
          feld(i) = feld(i) + dble(ilimit(i)  * ivz)
          !
       END IF
    !
    ENDDO
    !
    IF ( ilimit(1) <= 0 ) RETURN
    !
    d_res = ABS(feld(1))
    IF ( d_res >  DBLE(ilimit(1)-1) ) THEN
       !
       d_res = MODULO( d_res, DBLE(ilimit(1)) )
       !
    END IF
    !
  END SUBROUTINE r_bound_to_limits
  !
  !! Vergleichen zweier Datenobjekte this1 und this2
  !! Ergebnis:  1 : der Wert des Datenobjektes 1 ist groesser
  !!            0 : der Wert der Datenobjekte ist gleich
  !!           -1 : der Wert des Datenobjektes 2 ist groesser
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION vgl_komponenten ( this1, this2 ) &
         RESULT( vgl_res )
    !! Objekt 1 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_carry) , INTENT(IN) :: this2 ! 
    !! Vergleichsergebnis
    INTEGER :: vgl_res
    !! Vorzeichen
    INTEGER :: ivz ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ivz = ivorzeichen( size(this1%values), this1%values)
    !
    CALL bound_to_limits( ivz, this1 )
    !
    ivz = ivorzeichen( size(this2%values), this2%values)
    !
    CALL bound_to_limits( ivz, this2 )
    !
    vgl_res = 0
    !
    DO i = 1, size(this1%values)
       !
       IF ( this1%values(i) > this2%values(i) ) THEN
          !
          vgl_res = 1
          EXIT
          !
       ELSE IF ( this1%values(i) < this2%values(i) ) THEN
          !
          vgl_res = -1
          EXIT
          !
       END IF
       !
    END DO
    !
  END FUNCTION vgl_komponenten
  !
  !! Berechnet den Absolutwert eines Objektes "t_carry"
  FUNCTION absolute_value(this1) RESULT(this)
    !
    TYPE(t_carry), INTENT(in):: this1
    !
    TYPE(t_carry) :: this
    !
    INTEGER (kind=long):: ivz
    INTEGER (kind = long), POINTER, SAVE :: ifeld(:)
    !
    this%values => ifeld
    this%limits => this1%limits
    !
    ivz = ivorzeichen(SIZE(this1%values), this1%values)
    call set_carry(this, this1%values*ivz , this1%limits)
    !
  END FUNCTION absolute_value
  !
END MODULE b_carry
! TailOfBaseModule --------------------------------------------------------
